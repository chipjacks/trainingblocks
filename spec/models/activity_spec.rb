require 'rails_helper'

RSpec.describe Activity, type: :model do
  subject do
    create(:activity)
  end

  it "has a description, user, and data" do
    expect(subject.description).to be_truthy
    expect(subject.user).to be_truthy
    expect(subject.data).to be_truthy
  end

  describe ".from_strava_activity" do
    before :each do
      @user = create(:user)
    end

    it "sets activity data" do
      @import = create(:import, user: @user)
      result = Activity.from_strava_activity(@import)
      expect(result.data['duration']).to eq(@import.data['moving_time'])
      expect(result.data['pace']).to be_truthy
      expect(result.data['distance']).to be_truthy
      expect(result.data['completed']).to be_truthy
      expect(result.data['type']).to eq(Activity::RUN)
    end

    it "adds laps" do
      @import = create(:import, :laps, user: @user)
      result = Activity.from_strava_activity(@import)
      expect(result.data['laps'].length).to eq(10)
    end
  end
end
