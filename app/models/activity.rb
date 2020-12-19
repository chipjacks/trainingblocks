class Activity < ApplicationRecord
  belongs_to :user
  attr_accessor :date

  def self.list(user)
    activities = user.activities
    user.entries.map do |e|
      a = activities.find{ |a| a.id == e['id'] }.serializable_hash.deep_symbolize_keys
      a[:date] = e['date']
      a
    end
  end

  def self.merge(activities, strava_activities)
    unmatched = []
    strava_activities.map{ |s| from_strava_activity(s.as_json.deep_symbolize_keys) }.each do |s|
      matches = activities.select { |a| is_match(s, a) }
      if matches.empty? then
        unmatched.push(s)
      else
        matches.first[:strava_data] = s[:strava_data]
      end
    end

    activities.concat(unmatched)
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

    is_default_name = /^(Morning|Lunch|Afternoon|Evening|Night) \w+$/ =~ activity[:name]

    duration = activity[:moving_time] / 60
    data =
      if type === "run"
         { type: type, pace: "Easy", duration: duration, completed: true }
      else
         { type: type, duration: duration, completed: true }
      end

    { id: "strava.#{activity[:id]}", strava_data: activity, date: date, description: description, data: data }
  end

  def self.is_match(a, b)
    same_date = a[:date] == b[:date]

    same_type = a[:data][:type] == b[:data][:type]
    same_duration =
      if a[:data][:duration] && b[:data][:duration]
        (a[:data][:duration] - b[:data][:duration]).abs < 10
      else
        true
      end

    same_date && same_type && same_duration
  end
end
