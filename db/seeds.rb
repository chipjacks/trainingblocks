# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

def create_activity(obj, me)
  created = Activity.create(
    id: obj['id'],
    date: Date.parse(obj['date']),
    order: obj['order'],
    description: obj['description'],
    data: obj['data'],
    user: me
  )

end

me = User.find_by(uid: '2456610')

paces =
    [ {"pace"=>455, "name"=>"Very Easy"},
      {"pace"=>414, "name"=>"Easy"},
      {"pace"=>398, "name"=>"Moderate"},
      {"pace"=>380, "name"=>"Steady State"},
      {"pace"=>368, "name"=>"Brisk"},
      {"pace"=>353, "name"=>"Aerobic Threshold"},
      {"pace"=>338, "name"=>"Lactate Threshold"},
      {"pace"=>322, "name"=>"Groove"},
      {"pace"=>307, "name"=>"VO2 Max"},
      {"pace"=>292, "name"=>"Fast"} ]

if me.setting then
  me.setting.paces = paces
  me.setting.save
else
  me.create_setting({ paces: paces })
end

unless me then raise 'Strava uid not found.' end

me.activities.each{ |a| a.destroy! }

activities = JSON.parse(IO.read(File.join(Rails.root, 'db/activities.json')).chomp)
activities.each do |obj|
  create_activity(obj, me)
end

puts 'Success!'
