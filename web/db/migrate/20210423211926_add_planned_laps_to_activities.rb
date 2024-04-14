class AddPlannedLapsToActivities < ActiveRecord::Migration[6.1]
  class Activity < ActiveRecord::Base
  end

  def change
    Activity.reset_column_information
    Activity.all.each do |activity|
      activity.data = migrate_data(activity.data)
      activity.save!
    end
  end
end

def migrate_data(data)
  laps = data['laps'] || [data]

  has_planned_laps = laps.find { |l| !l['completed'] }

  completed_laps =
    laps.select { |l| l['completed'] || l.dig('laps', 0, 'completed') }

  new_data = {}
  new_data['laps'] = completed_laps

  if has_planned_laps
    new_data['planned'] =
      laps.map do |l|
        lap = Marshal.load(Marshal.dump(l))
        if lap['laps']
          lap['laps'].map { |l| l['completed'] = false }
        else
          lap['completed'] = false
        end
        lap
      end
  else
    new_data['planned'] = []
  end

  new_data
end
