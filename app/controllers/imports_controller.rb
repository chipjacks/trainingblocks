class ImportsController < ApplicationController
  before_action :authenticate_user!
  before_action :initialize_strava

  def index
    strava = @strava.get_logged_in_athlete_activities({per_page: 200})
    Import.add_new(strava, "strava", current_user)

    render json: { imports: current_user.imports }
  end

  private

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
