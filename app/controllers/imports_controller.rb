class ImportsController < ApplicationController
  skip_before_action :verify_authenticity_token, only: [:strava_push]

  def validate_strava_push
    render json: { "hub.challenge" => params["hub.challenge"] }
  end

  def strava_push
    user = User.find_by!(provider: Import::STRAVA, uid: params[:owner_id])
    if (params[:object_type] == "activity")
      import = Import.find_or_create(params[:object_id], Import::STRAVA, {}, user)
      import.stale = true
      import.save!
    else
      # TODO: handle api deauthorization
    end
    render json: { success: true }
  end

end
