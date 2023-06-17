require 'rails_helper'

RSpec.describe Activity, type: :model do
  subject { create(:activity) }

  it 'has a description, user, and data' do
    expect(subject.description).to be_truthy
    expect(subject.user).to be_truthy
    expect(subject.data).to be_truthy
  end

  describe '.from_strava_activity' do
    before :each do
      @user = create(:user)
    end

    it 'sets activity data' do
      @import = create(:import, user: @user)
      result = Activity.from_strava_activity(@import)

      lap = result.data['laps'][0]
      expect(lap['duration']).to eq(@import.data['moving_time'])
      expect(lap['pace']).to be_truthy
      expect(lap['distance']).to be_truthy
      expect(lap['completed']).to be_truthy
      expect(lap['type']).to eq(Activity::RUN)
    end

    it 'adds laps' do
      @import = create(:import, :laps, user: @user)
      result = Activity.from_strava_activity(@import)
      expect(result.data['laps'].length).to eq(10)
    end

    it 'handles missing paces' do
      @import = create(:import, :laps, user: @user)
      @import.data['laps'][0][:average_speed] = 0
      result = Activity.from_strava_activity(@import)
      expect(result.data['laps'].length).to eq(10)
    end
  end

  describe '#planned_duration' do
    it 'sums lap durations' do
      @import = create(:import, :laps)
      result = Activity.from_strava_activity(@import)
      expect(result.completed_duration).to be_within(10).of(
        @import.data['moving_time'],
      )
    end
  end

  describe '#run?' do
    it 'returns true if any planned or completed laps are runs' do
      activity = Activity.from_strava_activity(build(:import, :laps))
      expect(activity.run?).to eq(true)
      activity.data['laps'] << [{ type: Activity::OTHER, duration: 60 }]
      expect(activity.run?).to eq(true)
    end

    it 'returns false if no planned or completed laps are runs' do
      activity = Activity.from_strava_activity(build(:import, :laps))
      activity.data['laps'] =
        activity.data['laps'].map { |l| l['type'] = Activity::OTHER }
      expect(activity.run?).to eq(false)
    end
  end

  describe '#match_or_create' do
    it 'doesnt add new activity when an activity has already been created from that import' do
      import = create(:import, :laps)
      activity = Activity.from_strava_activity(import)
      activity.save!

      activity2 = Activity.from_strava_activity(import)
      expect { activity2.match_or_create }.not_to change { Activity.all.length }
    end

    it 'updates description when an activity has already been created from that import' do
      import = create(:import, :laps)
      activity = Activity.from_strava_activity(import)
      activity.save!

      import.data['name'] = 'New Name'
      activity2 = Activity.from_strava_activity(import)
      expect { activity2.match_or_create }.to change {
        activity.reload.description
      }
    end

    it 'creates a new activity when no matches found' do
      import = create(:import, :laps)
      activity = Activity.from_strava_activity(import)
      expect { activity.match_or_create }.to change { Activity.all.length }
    end

    it 'creates a new activity when a match is found that has a different import' do
      activity = Activity.from_strava_activity(create(:import, :laps))
      activity.save!
      activity2 =
        Activity.from_strava_activity(
          create(:import, :laps, id: '12345', user: activity.user),
        )
      activity2.id = '12345'
      expect { activity2.match_or_create }.to change { Activity.all.length }
    end

    it 'updates an existing activity when a match is found' do
      activity = Activity.from_strava_activity(build(:import, :laps))
      activity.data['planned'] = activity.data['laps']
      activity.data['laps'] = []
      activity.id = '12345'
      activity.import = nil
      activity.save!

      activity2 =
        Activity.from_strava_activity(
          create(:import, :laps, user: activity.user),
        )
      expect { activity2.match_or_create }.not_to change { Activity.all.length }
      activity.reload
      expect(activity.import).to be_truthy
      expect(activity.data['laps'].length > 0).to be_truthy
    end
  end
end
