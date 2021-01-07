# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

me = User.find_by(uid: "2456610")

unless me then raise "Strava uid not found." end

me.activities.each{ |a| a.destroy }

activities = JSON.parse(%x(cat db/activities.json | jq -cr '.').chomp)
activities.each do |obj|
  type = obj['data']['type']
  duration = obj['data']['duration']
  if type == "run" || type == "race"
    # convert minutes to seconds
    obj['data']['duration'] = duration * 60
  end
  Activity.create(
    id: obj["id"],
    description: obj["description"],
    data: obj["data"],
    user: me
  )
end

me.entries = JSON.parse(%x(cat db/entries.json | jq -cr '.').chomp)
me.save!
puts "Success!"
