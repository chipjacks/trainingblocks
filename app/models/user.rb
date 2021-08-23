class User < ApplicationRecord
  has_many :activities, dependent: :destroy
  has_many :imports, dependent: :destroy
  has_one :setting, dependent: :destroy

  after_save :initial_strava_import, if: :saved_change_to_uid?

  # Include default devise modules. Others available are:
  # :lockable, :timeoutable
  devise :database_authenticatable,
         :registerable,
         :confirmable,
         :recoverable,
         :rememberable,
         :trackable,
         :validatable,
         :omniauthable,
         omniauth_providers: %i[strava]

  def last_activity_update
    last_update =
      activities.unscoped.select(:updated_at).order(updated_at: :desc).limit(1)
    last_update.first ? last_update.first.updated_at.strftime('%s') : '0'
  end

  def self.from_omniauth(auth)
    where(provider: auth.provider, uid: auth.uid).first_or_create do |user|
      user.password = Devise.friendly_token[0, 20]
      user.auth_token = auth.credentials.refresh_token
    end
  end

  def merge_oauth(oauth_user)
    self.provider = oauth_user.provider
    self.uid = oauth_user.uid
    self.auth_token = oauth_user.auth_token
  end

  def self.new_with_session(params, session)
    super.tap do |user|
      if oauth_user = session[:oauth_user]
        user.provider = oauth_user['provider']
        user.uid = oauth_user['uid']
        user.auth_token = oauth_user['auth_token']
      end
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
      save!
      Rails.logger.debug "Strava token refreshed: #{res.body}"
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
    if provider == Import::STRAVA && auth_token
      InitialStravaImportJob.perform_now(self)
    end
  end
end
