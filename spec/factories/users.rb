FactoryBot.define do
  factory :user do
    email { "example@example.com" }
    password { "password" }
  end

  trait :activities do
    entries { JSON.parse(%x(cat db/entries.json | jq -cr '.').chomp) }

    after(:create) do |user, evaluator|
      activities = JSON.parse(%x(cat db/activities.json | jq -cr '.').chomp)
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
