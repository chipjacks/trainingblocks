class User < ApplicationRecord
  has_many :activities, dependent: :destroy
  has_many :imports, dependent: :destroy
  has_one :setting, dependent: :destroy
  attr_accessor :jwt_token

  after_save :initial_strava_import, if: :saved_change_to_uid?

  # Include default devise modules. Others available are:
  # :lockable, :timeoutable
  devise :database_authenticatable,
         :jwt_authenticatable,
         :registerable,
         :confirmable,
         :recoverable,
         :rememberable,
         :trackable,
         :validatable,
         :omniauthable,
         omniauth_providers: %i[strava],
         jwt_revocation_strategy: Devise::JWT::RevocationStrategies::Null # TODO

  def on_jwt_dispatch(token, payload)
    self.jwt_token = token
  end

  def last_activity_update
    last_update =
      activities.unscoped.select(:updated_at).order(updated_at: :desc).limit(1)
    last_update.first ? last_update.first.updated_at.strftime('%s') : '0'
  end

  def self.from_omniauth(auth)
    find_or_initialize_by(provider: auth.provider, uid: auth.uid) do |new_user|
        new_user.password = Devise.friendly_token[0, 20]
      end
      .tap do |user|
        user.auth_token = auth.credentials.refresh_token
        user.save
      end
  end

  def merge_oauth(oauth_user)
    self.provider = oauth_user.provider
    self.uid = oauth_user.uid
    self.auth_token = oauth_user.auth_token
  end

  def self.new_with_session(params, session)
    if !params.empty? && oauth_data = session[:oauth_user]
      oauth_user =
        find_or_initialize_by(
          provider: oauth_data['provider'],
          uid: oauth_data['uid'],
        )
      oauth_user.auth_token = oauth_data['auth_token']
      oauth_user.assign_attributes(params)
      oauth_user
    else
      super
    end
  end

  def get_strava_access_token
    uri = URI('https://www.strava.com/oauth/token')
    strava_config =
      Rails.configuration.devise.omniauth_configs[:strava].strategy
    data = {
      'client_id' => strava_config['client_id'],
      'client_secret' => strava_config['client_secret'],
      'grant_type' => 'refresh_token',
      'refresh_token' => auth_token,
    }

    res =
      Net::HTTP.post(uri, data.to_json, 'Content-Type' => 'application/json')

    case res
    when Net::HTTPSuccess
      json = JSON.parse(res.body)
      auth_token = json['refresh_token']
      if save(validate: false)
        Rails.logger.debug "Strava token refreshed: #{res.body}"
      else
        Rollbar.error('Error updating user auth_token', res.body)
      end
      return json['access_token']
    else
      Rails.logger.error "Error refreshing Strava token: #{res.body}"
      raise StravaClient::ApiError
    end
  end

  protected

  def email_required?
    true
  end

  def confirmation_required?
    true
  end

  def initial_strava_import
    if provider == Import::STRAVA && auth_token && imports.empty?
      InitialStravaImportJob.perform_now(self)
    end
  end
end
