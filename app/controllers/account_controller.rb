class AccountController < ApplicationController
  before_action :authenticate_user!

  def index
    respond_to do |format|
      format.html
      format.json { render json: current_user.as_json(only: required_fields) }
    end
  end

  def disconnect_strava
    current_user.provider = nil
    current_user.auth_token = nil
    current_user.uid = nil

    if current_user.save
      flash[:notice] = 'Strava account removed.'
      redirect_to account_path
    else
      flash[:error] =
        'An error occurred disconnecting your Strava account, please try again.'
      redirect_to account_path
    end
  end

  private

  def required_fields
    %w[email provider]
  end
end
