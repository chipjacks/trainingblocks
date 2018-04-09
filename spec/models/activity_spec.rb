require 'rails_helper'

RSpec.describe Activity, type: :model do
  it "has a working factory" do
    expect {
      FactoryBot.create(:activity)
    }.to change {
      Activity.count
    }.by(1)
  end
end
