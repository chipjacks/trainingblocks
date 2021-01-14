class ImportsController < ApplicationController
  before_action :authenticate_user!

  def index
    render json: { imports: current_user.imports }
  end

  private

end
