class ActivitiesController < ApplicationController
  before_action :authenticate_user!
  before_action :set_activity, only: [:update, :destroy]

  def index
    before = params.require(:before)
    external = fetch_external_activities(before.to_i)
    internal = current_user.activities.where("start_date <= ?", Time.at(before.to_i))
    render json: merge(external, internal)
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

  def merge(external, internal)
    transformed = external.map { |ea| helpers.transform_external_activity(ea) }
    (internal + transformed).sort{ |a, b| a.start_date <=> b.start_date }
  end

  def fetch_external_activities(before_timestamp)
    access_token = session["devise.strava_access_token"]
    StravaClient.configure { |config| config.access_token = access_token }
    strava = StravaClient::ActivitiesApi.new
    strava.get_logged_in_athlete_activities(before: before_timestamp)
  end

end