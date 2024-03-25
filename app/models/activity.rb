class Activity < ApplicationRecord
  belongs_to :user
  belongs_to :import, optional: true
  default_scope { order(date: :asc, order: :asc) }

  RUN = 'Run'
  OTHER = 'Other'

  DURATION_MATCH_THRESHOLD = 0.2

  def match_or_create
    match =
      Activity
        .where(date: self.date, user: self.user)
        .find { |a| self.match?(a) }

    # already created an activity from this import
    if match && match.id == self.id
      match.description = self.description
      match.save!
      match

      # no activity created yet, or the one that was created has a different import already
    elsif !match || (match && match.import)
      self.save!
      self

      # matching activity found without an import associated already
    elsif !match.import
      match.import = self.import
      match.data['laps'] = self.data['laps']
      match.save!
      match
    else
      nil
    end
  end

  def self.from_strava_activity(import)
    activity = import.data.deep_symbolize_keys

    date = Date.parse(activity[:start_date_local]).to_s
    description = activity[:name]
    data = {}

    if activity[:laps]
      data[:laps] = parse_strava_laps(activity[:laps], activity[:type])
    else
      data[:laps] = parse_strava_laps([activity], activity[:type])
    end

    activity_hash = {
      id: "#{activity[:id].to_s}",
      date: date,
      description: description,
      data: data,
      import: import,
      user: import.user,
    }

    Activity.new(activity_hash)
  end

  def run?
    all_laps = ((self.data['laps'] || []) + (self.data['planned'] || []))

    includes_a_run = all_laps.find { |l| l['type'] == Activity::RUN }

    !!includes_a_run
  end

  def planned_duration
    (self.data['planned'] || []).map { |l| l['duration'] || 0 }.sum
  end

  def completed_duration
    (self.data['laps'] || []).map { |l| l['duration'] || 0 }.sum
  end

  def match?(planned_activity)
    return true if self.id == planned_activity.id

    return false if planned_activity.completed_duration > 0

    same_date = self.date == planned_activity.date

    same_type =
      self.run? && planned_activity.run? ||
        (!self.run? && !planned_activity.run?)

    # planned duration must be threshold% more or less than completed duration
    same_duration =
      if self.completed_duration && planned_activity.planned_duration
        (self.completed_duration - planned_activity.planned_duration).abs <
          [10, DURATION_MATCH_THRESHOLD * planned_activity.planned_duration].max
      else
        false
      end

    same_date && same_type && same_duration
  end

  def update_strava_description(existing_description)
    if planned_duration > 0 && import
      append_description =
        "#{human_duration(planned_duration)} planned on https://runo.app"
      UpdateStravaDescriptionJob.perform_later(
        user,
        import.id,
        "#{existing_description}\n\n#{append_description}",
      )
    end
  end

  def human_duration(seconds)
    ActiveSupport::Duration
      .build(seconds)
      .parts
      .map do |key, value|
        if key == :seconds
          []
        else
          [value.to_i, key.to_s[0..-2].pluralize(value.to_i)]
        end
      end
      .flatten
      .join(' ')
  end

  private

  def self.to_seconds_per_mile(meters_per_second)
    (1609.3 / meters_per_second).round
  rescue FloatDomainError => e
    nil
  end

  def self.parse_strava_laps(laps, type)
    laps.map do |lap|
      type =
        if lap[:type] === RUN
          RUN
        elsif !lap[:type] && type === RUN
          RUN
        elsif !lap[:type] && !type
          RUN
        else
          OTHER
        end

      {
        type: type,
        pace: to_seconds_per_mile(lap[:average_speed]),
        distance: lap[:distance],
        duration: lap[:moving_time],
        elevationGain: lap[:total_elevation_gain],
        completed: true,
      }
    end
  end
end
