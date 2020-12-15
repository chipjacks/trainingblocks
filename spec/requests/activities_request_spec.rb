require 'rails_helper'

RSpec.describe "Activities", type: :request do
  before :each do
    @user = FactoryBot.create(:user)
    sign_in @user
    @activity = FactoryBot.build(:activity)
    @entries = [ { date: Date.today, id: @activity.id } ]
  end

  it "updates activities and entries" do
    post "/activities", :params => { changes: [ { msg: 'create', activity: @activity } ],
                                     entries: @entries,
                                     rev: Digest::MD5.hexdigest(@user.entries.to_json)
                                   }.as_json
    expect(response).to have_http_status(200)
    expect(@user.activities).to include(@activity)
    expect(@user.entries.to_json).to eq(@entries.to_json)
  end

  it "rejects stale updates" do
    post "/activities", :params => { changes: [ { msg: 'create', activity: @activity } ],
                                     entries: @entries,
                                     rev: 'incorrect_revision'
                                   }.as_json
    expect(response).to have_http_status(:conflict)
    expect(@user.activities).to_not include(@activity)
  end
end
