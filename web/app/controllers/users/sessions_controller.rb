# frozen_string_literal: true

class Users::SessionsController < Devise::SessionsController
  # before_action :configure_sign_in_params, only: [:create]
  before_action :drop_session_cookie

  # GET /resource/sign_in
  def new
    version_param = params[:v]
    @ios_auth = (version_param.present? && version_param == 'ios0.1')

    super
  end

  # POST /resource/sign_in
  def create
    super.tap do
      if current_user && !current_user.uid && oauth_user = session[:oauth_user]
        current_user.provider = oauth_user['provider']
        current_user.uid = oauth_user['uid']
        current_user.auth_token = oauth_user['auth_token']
        current_user.save!
      end
    end
  end

  # DELETE /resource/sign_out
  # def destroy
  #   super
  # end

  # protected

  # If you have extra params to permit, append them to the sanitizer.
  # def configure_sign_in_params
  #   devise_parameter_sanitizer.permit(:sign_in, keys: [:attribute])
  # end

  private

  def drop_session_cookie
    request.session_options[:skip] = true if request.params.dig(
      :user,
      :ios_auth,
    )
  end
end
