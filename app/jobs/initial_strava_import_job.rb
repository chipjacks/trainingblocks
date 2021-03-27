class InitialStravaImportJob < ApplicationJob
  queue_as :default
  retry_on StravaClient::ApiError

  def perform(user, access_token)
    StravaClient.configure { |config| config.access_token = access_token }
    strava_client = StravaClient::ActivitiesApi.new
    activities = strava_client.get_logged_in_athlete_activities({ per_page: 200 })

    Import.add_new(activities, Import::STRAVA, user)
  rescue StravaClient::ApiError => e
    logger.error "Strava API exception: #{e.message} #{e.response_body}"
    raise e
  end
end
