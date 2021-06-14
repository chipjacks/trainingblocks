class Users::OmniauthCallbacksController < Devise::OmniauthCallbacksController
  include Devise::Controllers::Rememberable

  def strava
    @user = User.from_omniauth(request.env["omniauth.auth"])
    credentials = request.env.dig("omniauth.auth", "credentials")

    if @user.persisted? && credentials
      if @user.previously_new_record? || !@user.auth_token
        InitialStravaImportJob.perform_now(@user, credentials["token"])
      end
      @user.auth_token = credentials["refresh_token"]
      remember_me(@user)
      @user.save!
      set_flash_message(:notice, :success, kind: "Strava") if is_navigational_format?
      store_location_for(@user, calendar_path)
      sign_in_and_redirect @user, event: :authentication
    else
      set_flash_message(:alert, :failure, kind: "Strava", reason: "#{@user.errors.full_messages.join(', ')}")
      redirect_to root_path
    end
  end

  def failure
    message = "Internal error"
    begin
      message = JSON.parse(request.env['omniauth.error'].message.delete_prefix(": "))["message"]
    rescue
    end
    set_flash_message(:alert, :failure, kind: "Strava", reason: message)
    redirect_to root_path
  end
end
