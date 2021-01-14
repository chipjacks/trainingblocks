class StravaImportJob < ApplicationJob
  queue_as :default

  def perform(user, access_token, strava_id)
    StravaClient.configure { |config| config.access_token = access_token }
    @strava_client = StravaClient::ActivitiesApi.new
    activity = @strava_client.get_activity_by_id(strava_id, { include_all_efforts: false })
    Import.add_new([activity], "strava", user)
  rescue StravaClient::ApiError => e
    logger.error "Exception in get_activity_by_id: #{e.message} #{e.response_body}"
  end
end
