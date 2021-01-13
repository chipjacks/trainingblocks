class Users::OmniauthCallbacksController < Devise::OmniauthCallbacksController
  def strava
    @user = User.from_omniauth(request.env["omniauth.auth"])
    credentials = request.env.dig("omniauth.auth", "credentials")

    if @user.persisted? && credentials
      if @user.previously_new_record?
        InitialStravaImportJob.perform_now(@user, credentials["token"])
      end
      sign_in_and_redirect @user, event: :authentication
      session["devise.strava_access_token"] = credentials["token"]
      session["devise.strava_refresh_token"] = credentials["refresh_token"]
      session["devise.strava_expires_at"] = credentials["expires_at"]
      set_flash_message(:notice, :success, kind: "Strava") if is_navigational_format?
    else
      set_flash_message(:alert, :failure, kind: "Strava", reason: "#{@user.errors.full_messages.join(', ')}")
      redirect_to root_path
    end
  end

  def failure
    redirect_to root_path
  end
end
