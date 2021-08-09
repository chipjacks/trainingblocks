class SettingsController < ApplicationController
  before_action :authenticate_user!

  def show
    current_user.create_setting if !current_user.setting

    respond_to do |format|
      format.html
      format.json do
        render json: current_user.setting.as_json(only: required_fields)
      end
    end
  end

  def update
    current_user.setting.update!(setting_params)

    render json: { ok: true }
  end

  private

  def setting_params
    params.permit(
      [{ paces: %i[name pace] }, :race_distance, :race_duration, :level]
    )
  end

  def required_fields
    %w[paces race_distance race_duration level]
  end
end
