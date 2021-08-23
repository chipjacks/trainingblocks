FactoryBot.define do
  factory :user do
    email { 'example@example.com' }
    password { 'password' }
    confirmed_at { Date.today }
  end

  trait :strava do
    after(:build) do |user|
      user.class.skip_callback(
        :save,
        :after,
        :initial_strava_import,
        raise: false,
      )
    end

    provider { Import::STRAVA }
    uid { 12_345_678 }
    auth_token { 'asdfghjkl' }
  end

  trait :activities do
    after(:create) do |user, evaluator|
      activities = JSON.parse(IO.read(Rails.root + 'db/activities.json').chomp)
      activities.each do |obj, idx|
        Activity.create(
          id: obj['id'],
          date: Date.parse(obj['date']),
          order: obj['order'],
          description: obj['description'],
          data: obj['data'],
          user: user,
        )
      end
    end
  end
end
