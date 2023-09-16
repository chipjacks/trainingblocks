class UpdateStravaDescriptionJob < ApplicationJob
  queue_as :default

  def perform(user, strava_activity_id, description)
    StravaClient.configure do |config|
      config.access_token = user.get_strava_access_token
    end
    strava_client = StravaClient::ActivitiesApi.new

    opts = {
      body: StravaClient::UpdatableActivity.new(description: description),
    }
    strava_client.update_activity_by_id(strava_activity_id, opts)
  rescue StravaClient::ApiError => e
    logger.error "Strava API exception: #{e.message} #{e.response_body}"
    raise e
  end
end
