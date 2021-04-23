class Activity < ApplicationRecord
  belongs_to :user
  belongs_to :import, optional: true
  default_scope { order(date: :asc, order: :asc) }

  RUN = "Run"
  OTHER = "Other"

  def match_or_create
    match = Activity.where(date: self.date, user: self.user).find { |a| self.match?(a) }
    if match && match.id == self.id
      nil
    elsif !match || (match && match.import)
      self.save!
    elsif !match.import
      match.import = self.import
      match.data['completed'] = true
      match.save!
    else
      nil
    end
  end

  def self.from_strava_activity(import)
    activity = import.data.deep_symbolize_keys

    date = Date.parse(activity[:start_date_local]).to_s
    description = activity[:name]
    data = {}

    if activity[:laps] then
      data[:laps] = parse_strava_laps(activity[:laps])
    else
      data[:laps] = parse_strava_laps([ activity ])
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

  def run?
    case self.data['type']
    when Activity::RUN
      true
    else
      false
    end
  end

  def planned_duration
    (self.data['planned'] || []).map {|l| l['duration'] || 0}.sum
  end

  def completed_duration
    (self.data['laps'] || []).map {|l| l['duration'] || 0}.sum
  end

  def match?(activity)
    return true if self.id == activity.id

    same_date = self.date == activity.date

    same_type = self.run? && activity.run? || (!self.run? && !activity.run?)

    ten_minutes = 10 * 60
    same_duration =
      if self.planned_duration && activity.completed_duration
        (self.planned_duration - activity.completed_duration).abs < ten_minutes
      else
        true
      end

    same_date && same_type && same_duration
  end

  private

  def self.to_seconds_per_mile(meters_per_second)
    (1609.3 / meters_per_second).round
  end

  def self.parse_strava_laps(laps)
    laps.map do |lap|
      type =
        if lap[:type] === RUN
          RUN
        elsif !lap[:type]
          RUN
        else
          OTHER
        end

      { type: type,
        pace: to_seconds_per_mile(lap[:average_speed]),
        distance: lap[:distance],
        duration: lap[:moving_time],
        completed: true
      }
    end
  end

end
