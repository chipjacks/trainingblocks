class ActivitiesController < ApplicationController
  before_action :authenticate_user!
  before_action :initialize_strava

  def index
    # activities = @strava.get_logged_in_athlete_activities({before: params['before'].to_i})
    # TODO: error handling
    render json: (current_user.activities || [])
  end

  def update
    current_user.activities = params[:activities].as_json
    result = current_user.save
    render json: { ok: result }
  end

  private

  def initialize_strava
    # TODO: make sure user is authenticated into Strava
    access_token = session["devise.strava_access_token"]
    StravaClient.configure { |config| config.access_token = access_token }
    @strava = StravaClient::ActivitiesApi.new
  end
end
