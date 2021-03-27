class UpdateStravaImportJob < ApplicationJob
  queue_as :default
  retry_on StravaClient::ApiError

  def perform(user, strava_activity_id)
    StravaClient.configure { |config| config.access_token = get_access_token(user) }
    strava_client = StravaClient::ActivitiesApi.new

    data = strava_client.get_activity_by_id(strava_activity_id, { include_all_efforts: false })
    import = Import.find_or_create(strava_activity_id, Import::STRAVA, data, user)
    import.data = data
    import.save!

    activity = Activity.from_strava_activity(import)
    activity.match_or_create

  rescue StravaClient::ApiError => e
    logger.error "Strava API exception: #{e.message} #{e.response_body}"
    raise e
  end

  def get_access_token(user)
    uri = URI('https://www.strava.com/oauth/token')
    strava_config = Rails.configuration.devise.omniauth_configs[:strava].strategy
    data = { 'client_id' => strava_config['client_id'],
             'client_secret' => strava_config['client_secret'],
             'grant_type' => 'refresh_token',
             'refresh_token' => user.auth_token
    }

    res = Net::HTTP.post(uri, data.to_json, "Content-Type" => "application/json")

    case res
    when Net::HTTPSuccess
      json = JSON.parse(res.body)
      Rails.logger.debug "Strava token refreshed: #{res.body}"
      return json["access_token"]
    else
      Rails.logger.error "Error refreshing Strava token: #{res.body}"
      raise StravaClient::ApiError
    end
  end
end
