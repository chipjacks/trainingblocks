class ChangeActivityDataToSeconds < ActiveRecord::Migration[6.1]
  def up
    Activity.all.each do |a|
      data = a.data
      if data['pace']
        data['trainingPace'] = data['pace']
        data['pace'] = trainingPaceToSeconds(data['pace'])
      end
      if data['duration'] && data['type'] != 'interval'
        data['minutes'] = data['duration']
        data['duration'] = data['duration'] * 60
      end
      a.data = data
      puts a.as_json
      a.save!
    end
  end

  def down
    Activity.all.each do |a|
      data = a.data
      if data['trainingPace']
        data['pace'] = data['trainingPace']
      end
      if data['minutes']
        data['duration'] = data['minutes']
      end
      a.data = data
      puts a.as_json
      a.save!
    end
  end
end


def trainingPaceToSeconds(tp)
  case tp
  when 'Easy'
    7 * 60 + 35
  when 'Moderate'
    6 * 60 + 44
  when 'Steady'
    6 * 60 + 29
  when 'Brisk'
    6 * 60 + 13
  when 'Aerobic'
    5 * 60 + 58
  when 'Lactate'
    5 * 60 + 43
  when 'Groove'
    5 * 60 + 27
  when 'VO2'
    5 * 60 + 12
  when 'Fast'
    4 * 60 + 56
  else
    7 * 60 + 45
  end
end
