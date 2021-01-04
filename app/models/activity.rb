class Activity < ApplicationRecord
  belongs_to :user
  attr_accessor :date

  def self.list(user)
    activities = user.activities
    entries = user.entries
    activityEntries = []
    entries.each do |e|
      a = activities.find{ |a| a.id == e['id'] }
      if !a
        Rails.logger.error "Missing entry: #{e}"
      else
        a = a.serializable_hash.deep_symbolize_keys
        a[:date] = e['date']
        activityEntries.push(a)
      end
    end
    activityEntries
  end

  def self.merge(user, strava_activities)
    activities = list(user)
    unmatched = []
    strava_activities.map{ |s| from_strava_activity(s.as_json.deep_symbolize_keys) }.each do |s|
      matches = activities.select { |a| is_match(s, a) }
      if matches.empty? then
        unmatched.push(s)
      end
    end

    entries = user.entries
    unmatched.each do |u|
      u[:user] = user
      a = u.reject {|k,v| k == :date }
      saved = Activity.create_with(a).find_or_create_by!(id: u[:id])
      find_or_create_entry(entries, u[:date], u[:id])
    rescue => exception
      Rails.logger.error "#{exception} - Unmatched activity: #{u}"
    end

    entries
  end

  def self.from_strava_activity(activity)
    type =
      if activity[:type] === "Run"
        "run"
      else
        "other"
      end

    date = Date.parse(activity[:start_date_local]).to_s
    description = activity[:name]
    duration = activity[:moving_time]
    pace = metersPerSecondToSecondsPerMile(activity[:average_speed])
    data =
      if type === "run"
         { type: type, pace: pace, duration: duration, completed: true }
      else
         { type: type, duration: duration, completed: true }
      end

    { id: activity[:id].to_s, date: date, description: description, data: data }
  end

  def self.is_match(a, b)
    return true if a[:id] == b[:id]

    same_date = a[:date] == b[:date]

    same_type = a[:data][:type] == b[:data][:type]
    ten_minutes = 10 * 60
    same_duration =
      if a[:data][:duration] && b[:data][:duration]
        (a[:data][:duration] - b[:data][:duration]).abs < ten_minutes
      else
        true
      end

    same_date && same_type && same_duration
  end

  def self.find_or_create_entry(entries, date, id)
    return if entries.find_index { |e| e['id'] == id }

    index = entries.index{ |e| Date.parse(e['date']) > Date.parse(date) }
    entries.insert(index || -1, { 'date' => date, 'id' => id })
  end

  def self.metersPerSecondToSecondsPerMile(mps)
    (1609.3 / mps).round
  end
end
