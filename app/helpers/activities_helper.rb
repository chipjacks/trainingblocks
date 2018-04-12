module ActivitiesHelper
  def transform_external_activity(a)
    Activity.new(
      { start_date: a.start_date,
        duration: a.moving_time,
        distance: a.distance,
        completed: true,
        external_id: a.id,
        type_: a.type,
        name: a.name
      }
    )
  end
end