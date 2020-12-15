class ActivitiesController < ApplicationController
  before_action :authenticate_user!
  before_action :initialize_strava

  def index
    # activities = @strava.get_logged_in_athlete_activities({before: params['before'].to_i})
    # TODO: error handling
    activities = Activity.list(current_user)
    render json: { activities: activities, rev: entries_revision }
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
          activity = current_user.activities.find(activity_params['id'])
          activity.update!(activity_params)
        elsif change['msg'] == 'delete'
          activity = current_user.activities.find(activity_params['id'])
          activity.destroy!()
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

    def initialize_strava
      # TODO: make sure user is authenticated into Strava
      access_token = session["devise.strava_access_token"]
      StravaClient.configure { |config| config.access_token = access_token }
      @strava = StravaClient::ActivitiesApi.new
    end

    def entries_revision
      Digest::MD5.hexdigest(current_user.reload.entries.to_json)
    end
end
