class HomeController < ApplicationController
    def index
        if current_user then
            render 'app'
        end
    end
end
