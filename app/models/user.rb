class User < ApplicationRecord
  has_many :activities, dependent: :destroy
  has_many :imports, dependent: :destroy
  has_one :setting, dependent: :destroy

  before_save :initial_strava_import, if: :will_save_change_to_uid?

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

  def self.new_with_session(params, session)
    super.tap do |user|
      if oauth_user = session[:oauth_user]
        user.provider = oauth_user['provider']
        user.uid = oauth_user['uid']
        user.auth_token = oauth_user['auth_token']
      end
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
      InitialStravaImportJob.perform_now(self, auth_token)
    end
  end
end
