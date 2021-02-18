class AddLapsToActivities < ActiveRecord::Migration[6.1]
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
  if data['type'] == 'session'
    data['laps'] = data['activities'].map do |a|
      migrate_data(a)
    end
    data.delete('activities')
  end

  type = data['type']
  duration = data['duration']

  if data['type'] == 'session'
    data['type'] = 'run'
    data['completed'] = true
  elsif data['type'] == 'run'
    data['effort'] = 'Easy'
  elsif data['type'] == 'interval'
    data['type'] = 'run'
    data['effort'] = 'Moderate'
  elsif data['type'] == 'race'
    data['type'] = 'run'
    data['effort'] = 'Hard'
    data['race'] = data['distance']
  elsif data['type'] == 'other' || data['type'] == 'note'
    data['type'] = 'other'
    data['completed'] = data['completed'] ? data['completed'] : true
  end

  data['type'] = data['type'].capitalize

  data
end
