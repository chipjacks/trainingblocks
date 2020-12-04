require 'rails_helper'

RSpec.describe Activity, type: :model do
  before :each do
    @activity = create(:activity)
  end

  it "has a description, date, and user" do
    expect(@activity.description).to be_truthy
    expect(@activity.date).to be_truthy
    expect(@activity.user).to be_truthy
  end
end
