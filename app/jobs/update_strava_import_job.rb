class UpdateStravaImportJob < ApplicationJob
  queue_as :default

  def perform(user, strava_activity_id, is_create = false)
    StravaClient.configure do |config|
      config.access_token = user.get_strava_access_token
    end
    strava_client = StravaClient::ActivitiesApi.new

    data =
      strava_client.get_activity_by_id(
        strava_activity_id,
        { include_all_efforts: false },
      )
    import =
      Import.find_or_create(strava_activity_id, Import::STRAVA, data, user)
    import.data = data
    import.save!

    activity = Activity.from_strava_activity(import)
    res = activity.match_or_create
    res.update_strava_description(import.data['description']) if is_create
  rescue StravaClient::ApiError => e
    logger.error "Strava API exception: #{e.message} #{e.response_body}"
    raise e
  end
end
