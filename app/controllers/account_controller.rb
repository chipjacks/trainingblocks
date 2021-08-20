class AccountController < ApplicationController
  before_action :authenticate_user!

  def index
    respond_to do |format|
      format.html
      format.json { render json: current_user.as_json(only: required_fields) }
    end
  end

  private

  def required_fields
    %w[email provider]
  end
end
