class Import < ApplicationRecord
  belongs_to :user
  has_one :activity

  STRAVA = "strava"

  def self.add_new(imports, source, user)
    ids = Hash[imports.map { |a| [a.id.to_s, a] }]
    existing_ids = user.imports.select(:id).map { |i| i.id }
    new_ids = Set.new(ids.keys) - existing_ids
    new_ids.map{ |id| ids[id] }.each do |import_data|
      import = find_or_create(import_data.id, source, import_data, user)
      activity = Activity.from_strava_activity(import)
      activity.match_or_create
    end
  end

  def self.find_or_create(id, source, data, user)
    hash =
      { id: id,
        source: source,
        data: data.as_json,
        user: user }

    Import.create_with(hash).find_or_create_by!(id: id)
  end
end
