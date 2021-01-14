class ActivitiesController < ApplicationController
  before_action :authenticate_user!

  def index
    render json: { activities: current_user.activities, rev: revision }
  end

  def batch_update
    rev = params[:rev]
    if (rev != revision)
      render status: :conflict, json: { ok: false }
      return
    end

    ActiveRecord::Base.transaction do
      params[:activityUpdates].each do |change|
        activity_params = change['activity'].permit(:id, :description, :date, data: {})

        case change['msg']
        when 'create'
          current_user.activities.create!(activity_params)
        when 'update'
          activity = current_user.activities.find(activity_params['id'])
          activity.update!(activity_params)
        when 'delete'
          activity = current_user.activities.find(activity_params['id'])
          activity.destroy!
        else
          raise ActiveRecord::StatementInvalid.new("Invalid change #{change}")
        end

      end

      (params[:orderUpdates] || []).each do |change|
        activity = Activity.find(change['id'])
        if !activity
          raise ActiveRecord::StatementInvalid.new("Invalid order #{change}")
        end
        activity.order = change['order']
        activity.save!
      end
    end

  rescue ActiveRecord::StatementInvalid => exception
    Rails.logger.error exception.message
    render status: :bad_request, json: { ok: false }
  rescue => exception
    Rails.logger.error exception.message
    render status: :internal_server_error, json: { ok: false }
  else
    render json: { ok: true, rev: revision }
  end

  private

    def revision
      current_user.last_activity_update
    end
end
