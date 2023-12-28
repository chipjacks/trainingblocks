class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  private

  def after_sign_in_path_for(resource)
    if resource.jwt_token && request.params.dig(:user, :ios_auth)
      'rhinolog://auth?token=' + resource.jwt_token
    else
      calendar_path
    end
  end

  def after_sign_out_path_for(resource_or_scope)
    root_path
  end
end
