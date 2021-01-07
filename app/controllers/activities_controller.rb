class ActivitiesController < ApplicationController
  before_action :authenticate_user!
  before_action :initialize_strava

  def index
    strava = @strava.get_logged_in_athlete_activities({per_page: 200})
    entries = Activity.merge(current_user, strava)

    Rails.logger.silence do
      current_user.entries = entries
      current_user.save!
    end

    current_user.reload

    render json: { activities: Activity.list(current_user), rev: entries_revision }
  end

  def batch_update
    rev = params[:rev]
    if (rev != entries_revision)
      render status: :conflict, json: { ok: false }
      return
    end

    changes = params[:changes]
    ActiveRecord::Base.transaction do
      changes.each do |change|
        activity_params = change['activity'].permit(:id, :description, :date, data: {})

        case change['msg']
        when 'create'
          current_user.activities.create!(activity_params)
        when 'update'
          activity = current_user.activities.find(activity_params['id'])
          activity.update!(activity_params)
        else
          raise ActiveRecord::StatementInvalid.new("Invalid change #{change}")
        end

      end

      Rails.logger.silence do
        current_user.entries = params[:entries]
        current_user.save!
      end
    end

  rescue ActiveRecord::StatementInvalid => exception
    Rails.logger.error exception.message
    render status: :bad_request, json: { ok: false }
  rescue => exception
    Rails.logger.error exception.message
    render status: :internal_server_error, json: { ok: false }
  else
    render json: { ok: true, rev: entries_revision }
  end

  private

    def entries_revision
      Digest::MD5.hexdigest(current_user.entries.to_json)
    end

    def initialize_strava
      # TODO: make sure user is authenticated into Strava
      refresh_strava_token
      access_token = session["devise.strava_access_token"]
      StravaClient.configure { |config| config.access_token = access_token }
      @strava = StravaClient::ActivitiesApi.new
    end

    def refresh_strava_token
      expiration_window_seconds = 3600
      refresh_token = session["devise.strava_refresh_token"]
      expires_at = session["devise.strava_expires_at"]

      if refresh_token && expires_at && (expires_at - Time.now().to_i < expiration_window_seconds)
        post_strava_refresh_token(refresh_token)
      end
    end

    def post_strava_refresh_token(refresh_token)
      uri = URI('https://www.strava.com/oauth/token')
      strava_config = Rails.configuration.devise.omniauth_configs[:strava].strategy
      data = { 'client_id' => strava_config['client_id'],
							 'client_secret' => strava_config['client_secret'],
							 'grant_type' => 'refresh_token',
							 'refresh_token' => refresh_token
			}

      res = Net::HTTP.post(uri, data.to_json, "Content-Type" => "application/json")

      case res
      when Net::HTTPSuccess
				json = JSON.parse(res.body)
        session["devise.strava_refresh_token"] = json["refresh_token"]
        session["devise.strava_expires_at"] = json["expires_at"]
        session["devise.strava_access_token"] = json["access_token"]
        Rails.logger.debug "Strava token refreshed: #{res.body}"
      else
        Rails.logger.error "Error refreshing Strava token: #{res.body}"
      end
    end
end
