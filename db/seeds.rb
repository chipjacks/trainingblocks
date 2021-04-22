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
    data: { laps: obj['data']['laps'] || [ obj['data'] ], planned: [] },
    user: me
  )

end

me = User.find_by(uid: '2456610')

unless me then raise 'Strava uid not found.' end

me.activities.each{ |a| a.destroy! }

activities = JSON.parse(IO.read(File.join(Rails.root, 'db/activities.json')).chomp)
activities.each do |obj|
  create_activity(obj, me)
end

puts 'Success!'
