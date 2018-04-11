module ActivitiesHelper
  def transform_external_activity(a)
    Activity.new(
      { start_date: a.start_date,
        duration: a.moving_time,
        distance: a.distance,
        completed: true,
        external_id: a.external_id
      }
    )
  end
end