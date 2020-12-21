class Activity < ApplicationRecord
  belongs_to :user
  attr_accessor :date

  def self.list(user)
    activities = user.activities
    activities = user.entries.map do |e|
      a = activities.find{ |a| a.id == e['id'] }.serializable_hash.deep_symbolize_keys
      a[:date] = e['date']
      a
    rescue => exception
      Rails.logger.error "#{exception} - Entry: #{e}"
      nil
    end
    activities.select{ |a| a }
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
      saved = Activity.create_with(a).find_or_create_by(id: u[:id])
      find_or_create_entry(entries, u[:date], u[:id])
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
    duration = activity[:moving_time] / 60
    data =
      if type === "run"
         { type: type, pace: "Easy", duration: duration, completed: true }
      else
         { type: type, duration: duration, completed: true }
      end

    { id: activity[:id].to_s, date: date, description: description, data: data }
  end

  def self.is_match(a, b)
    return true if a[:id] == b[:id]

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

  def self.find_or_create_entry(entries, date, id)
    return if entries.find_index { |e| e['id'] == id }

    index = entries.index{ |e| Date.parse(e['date']) > Date.parse(date) }
    entries.insert(index || -1, { 'date' => date, 'id' => id })
  end
end
