class HomeController < ApplicationController
    def index
      if current_user
        redirect_to calendar_url
      end
    end
end
