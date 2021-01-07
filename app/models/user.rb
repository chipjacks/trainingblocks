class User < ApplicationRecord
  has_many :activities, -> { order(date: :asc, order: :asc) }

  # Include default devise modules. Others available are:
  # :lockable, :timeoutable
  devise :database_authenticatable, :registerable, # :confirmable,
         :recoverable, :rememberable, :trackable, :validatable,
         :omniauthable, omniauth_providers: %i[strava]

  def last_activity_update
    last_update = activities.select(:updated_at).order(updated_at: :desc).limit(1)
    if last_update.first
      last_update.first.updated_at.to_s
    else
      "none"
    end
  end

  def self.from_omniauth(auth)
    where(provider: auth.provider, uid: auth.uid).first_or_create do |user|
      user.email = auth.info.email
      user.password = Devise.friendly_token[0,20]
    end
  end

  def self.new_with_session(params, session)
    super.tap do |user|
      if data = session["devise.strava_data"] && session["devise.strava_data"]["email"]
        user.email = data["email"] if user.email.blank?
      end
    end
  end

  protected

  def email_required?
    false
  end
end
