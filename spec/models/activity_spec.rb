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

  describe "#from_strava_activity" do
    before :each do
      @user = create(:user)
      @import = create(:import, user: @user)
    end

    it "sets activity data" do
      result = Activity.from_strava_activity(@import)
      expect(result.data['duration']).to eq(@import.data['moving_time'])
      expect(result.data['pace']).to be_truthy
      expect(result.data['completed']).to be_truthy
      expect(result.data['type']).to eq(Activity::RUN)
    end
  end
end
