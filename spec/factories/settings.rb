FactoryBot.define do
  factory :setting do
    user
    paces do
      [
        { 'pace' => 455, 'name' => 'Very Easy' },
        { 'pace' => 414, 'name' => 'Easy' },
        { 'pace' => 398, 'name' => 'Moderate' },
        { 'pace' => 380, 'name' => 'Steady State' },
        { 'pace' => 368, 'name' => 'Brisk' },
        { 'pace' => 353, 'name' => 'Aerobic Threshold' },
        { 'pace' => 338, 'name' => 'Lactate Threshold' },
        { 'pace' => 322, 'name' => 'Groove' },
        { 'pace' => 307, 'name' => 'VO2 Max' },
        { 'pace' => 292, 'name' => 'Fast' }
      ]
    end
  end
end
