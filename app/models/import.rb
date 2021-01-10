class Import < ApplicationRecord
  belongs_to :user
  belongs_to :activity, optional: true

  def self.from_strava_activity(activity, user)
    hash =
      { id: activity.id,
        source: "strava",
        data: activity.as_json,
        user: user }

    Import.create_with(hash).find_or_create_by!(id: hash[:id])
  end
end
