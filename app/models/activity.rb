class Activity < ApplicationRecord
  belongs_to :user
  belongs_to :import, optional: true
  default_scope { order(date: :asc, order: :asc) }

  RUN = "Run"
  OTHER = "Other"

  def match_or_create
    match = Activity.where(date: self.date, user: self.user).find { |a| self.match?(a) }
    if !match
      self.save!
    elsif !match.import
      match.import = self.import
      match.save!
    else
      nil
    end
  end

  def self.from_strava_activity(import)
    activity = import.data.deep_symbolize_keys
    type =
      if activity[:type] === RUN
        RUN
      else
        OTHER
      end

    date = Date.parse(activity[:start_date_local]).to_s
    description = activity[:name]
    duration = activity[:moving_time]
    data =
      if type === RUN
        pace = to_seconds_per_mile(activity[:average_speed])
        { type: type, pace: pace, duration: duration, completed: true }
      else
        { type: type, duration: duration, completed: true }
      end

    activity_hash =
      { id: "#{activity[:id].to_s}",
        date: date,
        description: description,
        data: data,
        import: import,
        user: import.user }

    Activity.new(activity_hash)
  end

  def match?(activity)
    return true if self.id == activity.id

    same_date = self.date == activity.date

    same_type = self.run? && activity.run? || (!self.run? && !activity.run?)
    ten_minutes = 10 * 60
    same_duration =
      if self.data['duration'] && activity.data['duration']
        (self.data['duration'] - activity.data['duration']).abs < ten_minutes
      elsif activity.data['type'] == 'session' && self.data['duration']
        (self.data['duration'] - activity.data['activities'].map{ |d| d['duration'] || 0 }.sum).abs < ten_minutes
      else
        true
      end

    same_date && same_type && same_duration
  end

  def run?
    case self.data['type']
    when 'run'
      true
    when 'interval'
      true
    when 'race'
      true
    when 'session'
      true
    else
      false
    end
  end


  def self.to_seconds_per_mile(meters_per_second)
    (1609.3 / meters_per_second).round
  end
end
