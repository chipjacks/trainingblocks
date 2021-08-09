def load_fixture(name)
  JSON.parse(IO.read(File.join(Rails.root, 'spec/factories', name)).chomp)
end

FactoryBot.define do
  factory :import do
    id { '4826872707' }
    source { 'strava' }
    data { load_fixture('strava_activity.json') }
    user

    trait :laps do
      data { load_fixture('strava_activity_with_laps.json') }
    end
  end
end
