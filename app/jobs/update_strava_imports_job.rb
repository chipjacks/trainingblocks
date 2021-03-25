class UpdateStravaImportsJob < ApplicationJob
  queue_as :default

  def perform(user, access_token)
    stale_imports = user.imports.where(stale: true);

    return if stale_imports.empty?

    StravaClient.configure { |config| config.access_token = access_token }
    @strava_client = StravaClient::ActivitiesApi.new

    stale_imports.each do |import|
      data = @strava_client.get_activity_by_id(import.id, { include_all_efforts: false })

      import.data = data
      import.stale = false
      import.save!

      activity = Activity.from_strava_activity(import)
      activity.match_or_create

    rescue StravaClient::ApiError => e
      logger.error "Exception in get_activity_by_id: #{e.message} #{e.response_body}"
    end
  end
end
