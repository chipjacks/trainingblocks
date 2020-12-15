class ActivitiesController < ApplicationController
  before_action :authenticate_user!
  before_action :initialize_strava

  def index
    # activities = @strava.get_logged_in_athlete_activities({before: params['before'].to_i})
    # TODO: error handling
    activities = Activity.list(current_user)
    render json: activities
  end

  def batch_update
    rev = params[:rev]
    if (rev != entries_revision)
      render status: :conflict, json: { ok: false }
      return
    end

    changes = params[:changes]
    Activity.transaction do
      changes.each do |change|
        activity_params = change['activity'].permit(:id, :description, :date, data: {})
        if change['msg'] == 'create'
          current_user.activities.create!(activity_params)
        elsif change['msg'] == 'update'
          current_user.activities.find(activity_params['id']).update!(activity_params)
        elsif change['msg'] == 'delete'
          current_user.activities.find(activity_params['id']).destroy!(activity_params)
        else
          raise ActiveRecord::StatementInvalid.new("Invalid change #{change}")
        end
      end

      current_user.entries = params[:entries]
      current_user.save!
    end

  rescue ActiveRecord::StatementInvalid => exception
    Rails.logger.error exception.message
    render status: :bad_request, json: { ok: false }
  rescue => exception
    Rails.logger.error exception.message
    render status: :internal_server_error, json: { ok: false }
  else
    render json: { ok: true }
  end

  private

    def initialize_strava
      # TODO: make sure user is authenticated into Strava
      access_token = session["devise.strava_access_token"]
      StravaClient.configure { |config| config.access_token = access_token }
      @strava = StravaClient::ActivitiesApi.new
    end

    def entries_revision
      Digest::MD5.hexdigest(current_user.entries.to_json)
    end
end
