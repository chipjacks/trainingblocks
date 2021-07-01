class SettingsController < ApplicationController
  before_action :authenticate_user!

  def index
    if !current_user.setting
      current_user.create_setting({ paces: []})
    end

    respond_to do |format|
      format.html
      format.json { render json: { paces: current_user.setting.paces } }
    end
  end

  def update
    current_user.setting.paces = params[:paces]
    current_user.setting.save

    render json: { ok: true }
  end
end
