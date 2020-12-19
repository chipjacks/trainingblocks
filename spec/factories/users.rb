FactoryBot.define do
  factory :user do
    email { "example@example.com" }
    password { "password" }
  end

  trait :activities do
    entries { JSON.parse(IO.read(Rails.root + 'db/entries.json').chomp) }

    after(:create) do |user, evaluator|
      activities = JSON.parse(IO.read(Rails.root + 'db/activities.json').chomp)
      activities.each do |obj|
        Activity.create(
          id: obj["id"],
          description: obj["description"],
          data: obj["data"],
          user: user
        )
      end

    end
  end
end
