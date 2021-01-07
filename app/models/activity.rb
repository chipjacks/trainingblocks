class Activity < ApplicationRecord
  belongs_to :user

  def self.merge(user, strava_activities)
    activities = user.activities
    unmatched = []
    strava_activities.map{ |s| from_strava_activity(s.as_json.deep_symbolize_keys) }.each do |s|
      matches = activities.select { |a| is_match(s, a) }
      if matches.empty? then
        unmatched.push(s)
      end
    end

    unmatched.each do |u|
      u[:user] = user
      saved = Activity.create_with(a).find_or_create_by!(id: u[:id])
    rescue => exception
      Rails.logger.error "#{exception} - Unmatched activity: #{u}"
    end
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
    data =
      if type === "run"
        pace = metersPerSecondToSecondsPerMile(activity[:average_speed])
        { type: type, pace: pace, duration: duration, completed: true }
      else
        { type: type, duration: duration, completed: true }
      end

    { id: "strava_#{activity[:id].to_s}", date: date, description: description, data: data }
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

  def self.metersPerSecondToSecondsPerMile(mps)
    (1609.3 / mps).round
  end
end
