class HomeController < ApplicationController
  def index
    redirect_to calendar_url if current_user
  end
end
