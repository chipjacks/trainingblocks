class InitialStravaImportJob < ApplicationJob
  queue_as :default

  def perform(user)
    StravaClient.configure do |config|
      config.access_token = user.get_strava_access_token
    end

    strava_client = StravaClient::ActivitiesApi.new
    activities =
      strava_client.get_logged_in_athlete_activities({ per_page: 200 })

    Import.add_new(activities, Import::STRAVA, user)
  end
end
