# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

def create_activity(obj, order, me)
  type = obj["data"]["type"]
  duration = obj["data"]["duration"]
  if type != "interval" && duration
    # convert minutes to seconds
    obj["data"]["duration"] = duration * 60
  end

  created = Activity.create(
    id: obj["id"].length == 10 ? obj["id"] : Array.new(10) { |i| rand(10) }.join,
    date: Date.parse(obj["date"]),
    order: order,
    description: obj["description"],
    data: obj["data"],
    user: me
  )

  if obj["data"]["type"] == "session"
    obj["data"]["activities"].each do |a|
      # create_activity(a, nil, me)
    end
  end
end

me = User.find_by(uid: "2456610")

unless me then raise "Strava uid not found." end

me.activities.each{ |a| a.destroy! }

activities = JSON.parse(%x(cat db/activities.json | jq -cr '.').chomp)
last_date = nil
order = 0
activities.each do |obj|
  create_activity(obj, order, me)

  if obj['date'] == last_date
    order += 1
  else
    order = 0
    last_date = obj['date']
  end
end

puts "Success!"
