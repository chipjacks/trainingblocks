class InitialStravaImportJob < ApplicationJob
  queue_as :default

  def perform(user, access_token)
    StravaClient.configure { |config| config.access_token = access_token }
    @strava_client = StravaClient::ActivitiesApi.new
    activities = @strava_client.get_logged_in_athlete_activities({ per_page: 200 })

    Import.add_new(activities, "strava", user)
  end
end
