require 'rails_helper'

RSpec.describe UpdateStravaImportJob, type: :job do
  include ActiveJob::TestHelper
  let(:user) { create(:user, :settings) }

  before :each do
    stub_request(:post, 'https://www.strava.com/oauth/token').to_return(
      body: '{}',
      headers: {
        content_type: 'application/json',
      },
    )
  end

  describe '#perform' do
    it 'creates new imports and activities' do
      import = build(:import)
      stub_activities_get(import)
      UpdateStravaImportJob.perform_now(user, import.id)
      expect(Import.find(import.id)).to be_truthy
      expect(Activity.find(import.id).completed_duration).to eq(
        import.data['moving_time'],
      )
    end

    it 'updates existing imports that have already been matched' do
      import = create(:import, user: user)
      activity = Activity.from_strava_activity(import)
      activity.id = "foo"
      activity.save!
      stub_activities_get(import)
      expect {
        UpdateStravaImportJob.perform_now(user, import.id)
      }.not_to change { Activity.all.length }
    end

    it 'updates strava description with planned duration' do
      import = build(:import, :laps, user: user)
      import.data['description'] = 'Great run on a nice day!'
      activity = Activity.from_strava_activity(import)
      activity.data['planned'] = activity.data['laps']
      activity.data['laps'] = []
      activity.id = '12345'
      activity.import = nil
      activity.save!
      stub_activities_get(import)
      UpdateStravaImportJob.perform_now(user, import.id, true)
      stub =
        stub_activities_put(
          import,
          {
            description:
              "Great run on a nice day!\n\n52 minutes planned on https://runo.app",
          },
        )
      perform_enqueued_jobs
      expect(a_request(:put, STRAVA_ACTIVITIES_API_PATH)).to have_been_requested
    end

    it 'doesnt update strava description if setting is false' do
      user.setting.strava_post = false
      user.save!
      import = build(:import, :laps, user: user)
      import.data['description'] = 'Great run on a nice day!'
      activity = Activity.from_strava_activity(import)
      activity.data['planned'] = activity.data['laps']
      activity.data['laps'] = []
      activity.id = '12345'
      activity.import = nil
      activity.save!
      stub_activities_get(import)
      UpdateStravaImportJob.perform_now(user, import.id, true)
      stub =
        stub_activities_put(import, { description: 'Great run on a nice day!' })
      perform_enqueued_jobs
      expect(
        a_request(:put, STRAVA_ACTIVITIES_API_PATH),
      ).not_to have_been_requested
    end
  end
end

def stub_activities_get(import)
  stub_request(:get, STRAVA_ACTIVITIES_API_PATH).to_return(
    body: import.data.to_json,
    headers: {
      content_type: 'application/json',
    },
  )
end

def stub_activities_put(import, expected_body)
  stub_request(:put, STRAVA_ACTIVITIES_API_PATH)
    .with(body: expected_body)
    .to_return(
      body: import.data.to_json,
      headers: {
        content_type: 'application/json',
      },
    )
end
