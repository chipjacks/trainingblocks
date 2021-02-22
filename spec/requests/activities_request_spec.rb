require 'rails_helper'

RSpec.describe "Activities", type: :request do
  before :each do
    @user = create(:user)
    sign_in @user
    @activity = build(:activity)
  end

  it "updates activities and entries" do
    post "/activities", :params => { activityUpdates: [ { msg: 'create', activity: @activity } ],
                                     rev: @user.last_activity_update
                                   }.as_json
    expect(response).to have_http_status(200)
    expect(@user.activities).to include(@activity)
  end

  it "rejects stale updates" do
    post "/activities", :params => { activityUpdates: [ { msg: 'create', activity: @activity } ],
                                     rev: 'incorrect_revision'
                                   }.as_json
    expect(response).to have_http_status(:conflict)
    expect(@user.activities).to_not include(@activity)
  end

  it "aborts invalid updates" do
    invalid_activity = { duration: 10 }
    post "/activities", :params => { activityUpdates: [ { msg: 'create', activity: invalid_activity } ],
                                     rev: @user.last_activity_update
                                   }.as_json
    expect(response).to have_http_status(:bad_request)
    expect(@user.activities).to_not include(invalid_activity)
  end
end
