class CalendarController < ApplicationController
  before_action :authenticate_user!

  def index
    @elm_flags = { user_id: current_user.id }
  end
end
