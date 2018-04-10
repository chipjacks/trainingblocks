class ActivitiesController < ApplicationController
  before_action :authenticate_user!
  before_action :set_activity, only: [:update, :destroy]

  def external
    access_token = session["devise.strava_access_token"]
    StravaClient.configure { |config| config.access_token = access_token }
    strava = StravaClient::ActivitiesApi.new
    activities = strava.get_logged_in_athlete_activities({before: params['before'].to_i})
    render json: activities
  end

  def index
    render json: current_user.activities
  end

  def create
    attrs = activity_params.merge(user_id: current_user.id)
    activity = current_user.activities.create!(attrs)
    render json: activity
  end

  def update
    @activity.update(activity_params)
    head :no_content
  end

  def destroy
    @activity.destroy
    head :no_content
  end

  private

  def activity_params
    params.require(:activity).permit(:start_date, :duration, :distance, :completed, :external_id)
  end

  def set_activity
    @activity = current_user.activities.find_by!(id: params[:id])
  end

end
