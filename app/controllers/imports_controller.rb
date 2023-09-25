class ImportsController < ApplicationController
  skip_before_action :verify_authenticity_token, only: [:strava_push]

  def validate_strava_push
    render json: { 'hub.challenge' => params['hub.challenge'] }
  end

  def strava_push
    user = User.find_by!(provider: Import::STRAVA, uid: params[:owner_id])
    if (params[:object_type] == 'activity')
      if (params[:aspect_type] == 'delete')
        import = Import.find(params[:object_id])
        import.destroy!
      else
        # "create" or "update"
        UpdateStravaImportJob.perform_later(
          user,
          params[:object_id],
          params[:aspect_type] == 'create',
        )
      end
    else
      # TODO: handle api deauthorization
    end
    render json: { success: true }
  end
end
