require 'rails_helper'

RSpec.describe ActivitiesHelper do
  describe "#transform_external_activity" do
    it 'transforms an activity' do
      external = StravaClient::SummaryActivity.new(JSON.parse(external_fixture('strava_activities.json')).first)
      internal = transform_external_activity(external)
      expect(internal).to be_a Activity
      expect(internal.slice(*%w(start_date type_ name duration distance completed external_id)).values).to_not include nil
    end
  end
end